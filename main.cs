using System;

public class ProductMatching {
	
	string DataPath = @"F:\Projects\Sortable";
	
	class Listing
	{
		public string title;
		public string manufacturer;
		public string currency;
		public string price;
	}
	
	class Product
	{
		public string product_name;
		public string manufacturer;
		public string model;
		public string family;
		public string announced_date;
	}
	
	class Result 
	{
		public string product_name;
		public List<Listing> listings;
		
		public Result(string pn, List<Listing> l) {
			this.product_name = pn;
			this.listings = l;
		}
	}
	
	bool containsI(string a, string b) {
		return a.IndexOf(b, StringComparison.OrdinalIgnoreCase) >= 0;
	}
	
	bool containsWholeWord(string a, string b) {
		//most common case - doesn't exist
		var idx = a.IndexOf(b, StringComparison.OrdinalIgnoreCase);
		if(idx < 0) return false;
	
		//next most common case - exists with spaces on both sides
		var idxSpaces = a.IndexOf(" " + b + " ", StringComparison.OrdinalIgnoreCase);
		if(idxSpaces >= 0) return true;
		
		//other cases - at beginning or end
		if(idx == 0 && a[b.Length] == ' ') return true;
		if(idx == a.Length - b.Length && a[a.Length - b.Length - 1] == ' ') return true;
		
		return false;
	}
	
	void Main()
	{
		var listingLines = File.ReadAllLines(Path.Combine(DataPath,"listings.txt"));
		var listings = listingLines.Select(JsonConvert.DeserializeObject<Listing>).ToList();
	
		var productLines = File.ReadAllLines(Path.Combine(DataPath,"products.txt"));
		var products = productLines.Select(JsonConvert.DeserializeObject<Product>).ToList();
	
		// - Match manufacturer
		// - Match family, account for spaces and dashes being interchangeable
		// - Match model, account for spaces and dashes, match only whole words to account for case where a model is a substring of another
		// - Future: remove accessories, account for models with extra letters at the end
		
		var result = products.Select(product => {
			List<string> altModel = new List<string>();
			if(product.model == null) {
				altModel.Add("");
			} else {
				altModel.Add(product.model);
				altModel.Add(product.model.Replace("-", " "));
				altModel.Add(product.model.Replace(" ", "-"));
				altModel.Add(product.model.Replace("-", ""));
				altModel.Add(product.model.Replace(" ", ""));
			}
			var altModelD = altModel.Distinct();
			
			List<string> altFamily = new List<string>();
			if(product.family == null) { 
				altFamily.Add(""); 
			} else {
				altFamily.Add(product.family);
				altFamily.Add(product.family.Replace("-", " "));
				altFamily.Add(product.family.Replace(" ", "-"));
				altFamily.Add(product.family.Replace("-", ""));
				altFamily.Add(product.family.Replace(" ", ""));
			}
			var altFamilyD = altFamily.Distinct();
			
			var p1listings = listings.Where(i => containsI(i.manufacturer, product.manufacturer) 
											&& (altModelD.Any(m => containsWholeWord(i.title, m))) 
											&& (altFamilyD.Any(f => containsI(i.title, f))));
											
			return p1listings;
		});
		
		var results = products.Zip(result, (product, presult) => new Result(product.product_name, presult.ToList())); 
		
		var serialresults = results.Select(i => JsonConvert.SerializeObject(i));
		File.WriteAllLines(Path.Combine(DataPath,"results.txt"), serialresults);
	
	}
}
